use serde::Deserialize;

#[derive(Clone, Deserialize)]
pub(crate) struct Root {
    pub(crate) package: Option<Package>,
    pub(crate) lib: Option<Lib>,
}

#[derive(Clone, Deserialize)]
pub(crate) struct Package {
    pub(crate) name: Option<String>,
    pub(crate) version: Option<String>,
}

#[derive(Clone, Deserialize)]
pub(crate) struct Lib {
    #[serde(alias = "crate-type")]
    pub(crate) crate_type: Option<Vec<String>>,
}
