use clap::Parser;
use miette::{
    GraphicalReportHandler, GraphicalTheme, IntoDiagnostic, NamedSource, ThemeCharacters,
    ThemeStyles,
};
use rcc::{codegen::X86Emitter, lexer::Lexer, tacky::TackyEmitter};
use std::io::Write;
use std::{fs::File, path::PathBuf, process::Command};

#[derive(clap::Parser, Debug)]
#[command(author, version, about)]
struct Args {
    #[arg(value_name = "FILE", required = true)]
    c_files: Vec<PathBuf>,

    /// Stop after lexing
    #[arg(long, conflicts_with_all = ["parse", "tacky", "codegen", "validate"])]
    lex: bool,

    /// Stop after parsing
    #[arg(long, conflicts_with_all = ["lex", "tacky", "codegen", "validate"])]
    parse: bool,

    /// Stop after semantic analysis
    #[arg(long, conflicts_with_all = ["lex", "parse", "tacky", "codegen"])]
    validate: bool,

    /// Stop after TACKY emission
    #[arg(long, conflicts_with_all = ["lex", "parse", "codegen", "validate"])]
    tacky: bool,

    /// Stop after codegen
    #[arg(long, conflicts_with_all = ["lex", "parse", "tacky", "validate"])]
    codegen: bool,

    /// Output file
    #[arg(short = 'o', long = "output", value_name = "FILE")]
    output: Option<PathBuf>,

    /// Compile, do not link
    #[arg(short = 'c')]
    compile: bool,
}

fn main() -> miette::Result<()> {
    let args = Args::parse();

    if args.c_files.is_empty() {
        eprintln!("error: at least one input file required");
        std::process::exit(1);
    }

    for c_file in &args.c_files {
        if !c_file.is_file() {
            eprintln!("error: '{}' is not a valid file", c_file.display());
            std::process::exit(1);
        }
    }

    if args.lex || args.parse || args.validate || args.tacky || args.codegen {
        for c_file in &args.c_files {
            let prog = std::fs::read_to_string(c_file).unwrap_or_else(|e| {
                eprintln!("error reading '{}': {e}", c_file.display());
                std::process::exit(1);
            });

            let asm_path = c_file.with_extension("s");
            let out_path = args
                .output
                .clone()
                .unwrap_or_else(|| c_file.with_extension(""));

            run(&args, &prog, &asm_path, &out_path, c_file)?;
        }
        return Ok(());
    }

    let mut object_files = Vec::new();
    for c_file in &args.c_files {
        let prog = std::fs::read_to_string(c_file).unwrap_or_else(|e| {
            eprintln!("error reading '{}': {e}", c_file.display());
            std::process::exit(1);
        });

        let asm_path = c_file.with_extension("s");
        let obj_path = c_file.with_extension("o");

        compile(&args, &prog, &asm_path, c_file)?;
        assemble(&asm_path, &obj_path)?;

        object_files.push(obj_path);

        std::fs::remove_file(&asm_path).into_diagnostic()?;
    }

    let final_output = args
        .output
        .clone()
        .unwrap_or_else(|| args.c_files[0].with_extension(""));

    link(&object_files, &final_output, args.compile)?;

    if !args.compile {
        for obj_file in &object_files {
            let _ = std::fs::remove_file(obj_file);
        }
    }

    Ok(())
}

fn compile(
    _args: &Args,
    prog: &str,
    asm_path: &std::path::Path,
    c_file: &std::path::Path,
) -> miette::Result<()> {
    let source = NamedSource::new(c_file.display().to_string(), prog.to_string());

    // Set up custom miette reporter with minimal theme
    miette::set_hook(Box::new(|_| {
        Box::new(
            GraphicalReportHandler::default().with_theme(GraphicalTheme {
                characters: ThemeCharacters {
                    hbar: ' ',
                    vbar: ' ',
                    xbar: ' ',
                    vbar_break: ' ',
                    ltop: ' ',
                    rtop: ' ',
                    mtop: ' ',
                    lbot: ' ',
                    rbot: ' ',
                    mbot: ' ',
                    error: "->".into(),
                    warning: "".into(),
                    advice: "".into(),
                    ..ThemeCharacters::ascii()
                },
                styles: ThemeStyles::rgb(),
            }),
        )
    }))?;

    let _tokens = Lexer::new(prog)
        .lex()
        .map_err(|e| miette::Report::new(e).with_source_code(source.clone()))?;

    let mut ast = rcc::parser::Parser::new(prog)
        .parse()
        .map_err(|e| miette::Report::new(e).with_source_code(source.clone()))?;

    let mut sema = rcc::sema::Sema::new();
    sema.run(&mut ast)
        .map_err(|e| miette::Report::new(e).with_source_code(source.clone()))?;

    let mut tacky_emitter = TackyEmitter::new();
    tacky_emitter
        .visit_program(&ast)
        .map_err(|e| miette::Report::new(e).with_source_code(source.clone()))?;
    let prog = tacky_emitter
        .get_program()
        .expect("There should be a program");

    let mut emitter = X86Emitter::new();
    emitter.visit_program(prog).into_diagnostic()?;
    let prog = emitter.get_program().expect("There should be a program");

    let mut file = File::create(asm_path).into_diagnostic()?;
    writeln!(file, "{}", prog).into_diagnostic()?;

    Ok(())
}

fn assemble(asm_path: &std::path::Path, obj_path: &std::path::Path) -> miette::Result<()> {
    let status = Command::new("clang")
        .arg("-c")
        .arg(asm_path)
        .arg("-o")
        .arg(obj_path)
        .status()
        .into_diagnostic()?;

    if !status.success() {
        miette::bail!("Error assembling {}", asm_path.display());
    }

    Ok(())
}

fn link(
    object_files: &[PathBuf],
    output: &std::path::Path,
    compile_only: bool,
) -> miette::Result<()> {
    if compile_only {
        return Ok(());
    }

    let mut cmd = Command::new("clang");
    for obj_file in object_files {
        cmd.arg(obj_file);
    }
    cmd.arg("-o").arg(output);

    let status = cmd.status().into_diagnostic()?;
    if !status.success() {
        miette::bail!("Error linking object files");
    }

    Ok(())
}

fn run(
    args: &Args,
    prog: &str,
    asm_path: &std::path::Path,
    out_path: &std::path::Path,
    c_file: &std::path::Path,
) -> miette::Result<()> {
    let source = NamedSource::new(c_file.display().to_string(), prog.to_string());

    // Set up custom miette reporter with minimal theme
    miette::set_hook(Box::new(|_| {
        Box::new(
            GraphicalReportHandler::default().with_theme(GraphicalTheme {
                characters: ThemeCharacters {
                    hbar: ' ',
                    vbar: ' ',
                    xbar: ' ',
                    vbar_break: ' ',
                    ltop: ' ',
                    rtop: ' ',
                    mtop: ' ',
                    lbot: ' ',
                    rbot: ' ',
                    mbot: ' ',
                    error: "->".into(),
                    warning: "".into(),
                    advice: "".into(),
                    ..ThemeCharacters::ascii()
                },
                styles: ThemeStyles::rgb(),
            }),
        )
    }))?;

    let tokens = Lexer::new(prog)
        .lex()
        .map_err(|e| miette::Report::new(e).with_source_code(source.clone()))?;
    if args.lex {
        tokens.iter().for_each(|tok| print!("{}", tok));
        println!();
        return Ok(());
    }

    let mut ast = rcc::parser::Parser::new(prog)
        .parse()
        .map_err(|e| miette::Report::new(e).with_source_code(source.clone()))?;
    if args.parse {
        println!("{:?}", ast);
        return Ok(());
    }

    let mut sema = rcc::sema::Sema::new();
    sema.run(&mut ast)
        .map_err(|e| miette::Report::new(e).with_source_code(source.clone()))?;
    if args.validate {
        println!("{:?}", ast);
        return Ok(());
    }

    let mut tacky_emitter = TackyEmitter::new();
    tacky_emitter
        .visit_program(&ast)
        .map_err(|e| miette::Report::new(e).with_source_code(source.clone()))?;
    let prog = tacky_emitter
        .get_program()
        .expect("There should be a program");
    if args.tacky {
        println!("{}", prog);
        return Ok(());
    }

    let mut emitter = X86Emitter::new();
    emitter.visit_program(prog).into_diagnostic()?;
    let prog = emitter.get_program().expect("There should be a program");
    if args.codegen {
        println!("{}", prog);
        return Ok(());
    }

    let mut file = File::create(asm_path).into_diagnostic()?;
    writeln!(file, "{}", prog).into_diagnostic()?;

    let status = Command::new("clang")
        .arg(asm_path)
        .arg(if args.compile { "-c" } else { "" })
        .arg("-o")
        .arg(out_path)
        .status()
        .into_diagnostic()?;

    if !status.success() {
        miette::bail!("Error linking program");
    }

    std::fs::remove_file(asm_path).into_diagnostic()?;

    Ok(())
}
