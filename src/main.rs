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
    #[arg(value_name = "FILE")]
    c_file: PathBuf,

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
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    if !args.c_file.is_file() {
        eprintln!("error: input must be a file");
        std::process::exit(1);
    }

    let prog = match std::fs::read_to_string(&args.c_file) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(1);
        }
    };

    let asm_path = args.c_file.with_extension("s");
    let out_path = args
        .output
        .clone()
        .unwrap_or_else(|| args.c_file.with_extension(""));

    run(&args, &prog, &asm_path, &out_path)?;

    Ok(())
}

fn run(
    args: &Args,
    prog: &str,
    asm_path: &std::path::Path,
    out_path: &std::path::Path,
) -> miette::Result<()> {
    let source = NamedSource::new(args.c_file.display().to_string(), prog.to_string());

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
