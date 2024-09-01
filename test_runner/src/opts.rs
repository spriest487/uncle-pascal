use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
pub struct Opts {
    #[structopt(name = "use-release", default_value = "false", parse(try_from_str))]
    pub use_release: bool,

    #[structopt(name = "error_continue", default_value = "false", parse(try_from_str))]
    pub error_continue: bool,
}
