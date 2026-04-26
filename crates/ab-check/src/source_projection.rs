use regex::Regex;

pub fn comparison_lossy_body(txt: &str) -> String {
    let gaiji = Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap();
    let explicit_ruby = Regex::new(r"｜([^《》\r\n]+)《[^》]+》").unwrap();
    let ruby = Regex::new(r"｜?([^｜\s《》※［＃\[\]］、。，．「」『』（）()]+)《[^》]+》").unwrap();
    let orphan_ruby = Regex::new(r"《[^》]+》").unwrap();
    let command = Regex::new(r"［＃[^］]+］|\[#[^\]]+\]").unwrap();
    let without_gaiji = gaiji.replace_all(txt, "");
    let without_explicit_ruby = explicit_ruby.replace_all(&without_gaiji, "$1");
    let without_ruby = ruby.replace_all(&without_explicit_ruby, "$1");
    let without_orphan_ruby = orphan_ruby.replace_all(&without_ruby, "");
    command.replace_all(&without_orphan_ruby, "").into_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn comparison_lossy_body_excludes_unresolved_gaiji_descriptions() {
        let visible = comparison_lossy_body("二二※［＃小書き片仮名ン、237-11］が四");

        assert_eq!(visible, "二二が四");
    }

    #[test]
    fn comparison_lossy_body_projects_explicit_ruby_base_without_marker() {
        let visible =
            comparison_lossy_body("――『｜あのひとにとって、わたし《ルビ》はなんだろう？」");

        assert_eq!(visible, "――『あのひとにとって、わたしはなんだろう？」");
    }

    #[test]
    fn comparison_lossy_body_removes_orphan_ruby_after_unresolved_gaiji() {
        let visible =
            comparison_lossy_body("ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも");

        assert_eq!(visible, "ことを、にも");
    }
}
