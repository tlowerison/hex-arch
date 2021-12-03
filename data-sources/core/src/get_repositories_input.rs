use pcre2::bytes::Regex;
use repositories_core::RepositoriesInput;
use std::path::PathBuf;
use syn::LitStr;

lazy_static! {
    static ref REPOSITORIES_MACRO_INSTANCE_RE: Regex = Regex::new(r"repositories! \{\K(?<e>[^{}]*(?:\{(?&e)\}[^{}]*)*)(?=\})").unwrap();
}

pub (crate) fn get_repositories_input(path: &LitStr, relative_path_to_source_file: PathBuf) -> RepositoriesInput {
    let mut path_to_source_file = std::env::current_dir().unwrap();
    path_to_source_file.push(relative_path_to_source_file.parent().unwrap());
    path_to_source_file.push(path.value());

    let file = std::fs::read_to_string(path_to_source_file).unwrap_or_else(|err| syn::Error::new_spanned(path, err).to_string());

    let mut matches: Vec<_> = REPOSITORIES_MACRO_INSTANCE_RE.find_iter(file.as_bytes()).collect();

    let repositories_macro_instance_tokens = if matches.len() == 0 {
        panic!("no instances of repositories macro found in \"{}\"; note that this macro expects exactly one space in the macro instance, i.e. \"repositories! {{\"", path.value());
    } else if matches.len() > 1 {
        panic!("multiple instances of repositories macro found in \"{}\", unable to proceed; try moving each instance of repositories into a separate file", path.value());
    } else {
        let m = matches
            .pop()
            .unwrap()
            .unwrap();
        String::from(&file[m.start()..m.end()])
    };

    syn::parse_str(&repositories_macro_instance_tokens).unwrap()
}
