use clap::Parser;
use rcc::{
    codegen::{X86Emitter, X86EmitterError},
    lexer::{Lexer, LexerError},
    parser::ParserError,
    sema::SemaError,
    tacky::{TackyEmitter, TackyError},
};
use std::io::Write;
use std::{error::Error, fmt::Display, fs::File, path::PathBuf, process::Command};

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

fn main() {
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

    if let Err(e) = run(&args, &prog, &asm_path, &out_path) {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}

fn run<'a>(
    args: &Args,
    prog: &'a str,
    asm_path: &std::path::Path,
    out_path: &std::path::Path,
) -> Result<(), CompileError<'a>> {
    let tokens = Lexer::new(prog).lex()?;
    if args.lex {
        tokens.iter().for_each(|tok| print!("{}", tok));
        println!();
        return Ok(());
    }

    let mut ast = rcc::parser::Parser::new(prog).parse()?;
    if args.parse {
        println!("{:?}", ast);
        return Ok(());
    }

    let mut sema = rcc::sema::VariableResolver::new();
    sema.run(&mut ast)?;
    if args.validate {
        println!("{:?}", ast);
        return Ok(());
    }

    let mut tacky_emitter = TackyEmitter::new();
    tacky_emitter.visit_program(ast)?;
    let prog = tacky_emitter
        .get_program()
        .expect("There should be a program");
    if args.tacky {
        println!("{}", prog);
        return Ok(());
    }

    let mut emitter = X86Emitter::new();
    emitter.visit_program(prog)?;
    let prog = emitter.get_program().expect("There should be a program");
    if args.codegen {
        println!("{}", prog);
        return Ok(());
    }

    let mut file = File::create(asm_path)?;
    writeln!(file, "{}", prog)?;

    let status = Command::new("clang")
        .arg(asm_path)
        .arg("-o")
        .arg(out_path)
        .status()?;

    if !status.success() {
        return Err(CompileError::Linker);
    }

    std::fs::remove_file(asm_path)?;

    Ok(())
}

#[derive(Debug)]
enum CompileError<'a> {
    Io(std::io::Error),
    Lexer(LexerError<'a>),
    Parser(ParserError<'a>),
    Sema(SemaError),
    Tacky(TackyError),
    CodeGen(X86EmitterError),
    Linker,
}

impl<'a> From<LexerError<'a>> for CompileError<'a> {
    fn from(e: LexerError<'a>) -> Self {
        CompileError::Lexer(e)
    }
}

impl<'a> From<ParserError<'a>> for CompileError<'a> {
    fn from(e: ParserError<'a>) -> Self {
        CompileError::Parser(e)
    }
}

impl From<std::io::Error> for CompileError<'_> {
    fn from(e: std::io::Error) -> Self {
        CompileError::Io(e)
    }
}

impl From<X86EmitterError> for CompileError<'_> {
    fn from(e: X86EmitterError) -> Self {
        CompileError::CodeGen(e)
    }
}

impl From<TackyError> for CompileError<'_> {
    fn from(e: TackyError) -> Self {
        CompileError::Tacky(e)
    }
}

impl From<SemaError> for CompileError<'_> {
    fn from(e: SemaError) -> Self {
        CompileError::Sema(e)
    }
}

impl Error for CompileError<'_> {}

impl Display for CompileError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Io(e) => e.fmt(f),
            CompileError::Lexer(e) => e.fmt(f),
            CompileError::Parser(e) => e.fmt(f),
            CompileError::Sema(e) => e.fmt(f),
            CompileError::Tacky(e) => e.fmt(f),
            CompileError::CodeGen(e) => e.fmt(f),
            CompileError::Linker => write!(f, "Error linking program"),
        }
    }
}
