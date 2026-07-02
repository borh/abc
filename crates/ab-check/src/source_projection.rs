#[must_use]
pub fn comparison_lossy_body(txt: &str) -> String {
    ab_source_syntax::comparison_lossy_body(txt).into_owned()
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

    #[test]
    fn comparison_lossy_body_removes_nested_gaiji_in_command() {
        let visible = comparison_lossy_body(
            "豌豆《ゑんどう》［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］の大さ",
        );

        assert_eq!(visible, "豌豆の大さ");
    }

    #[test]
    fn comparison_lossy_body_removes_unmatched_ruby_delimiters() {
        let visible = comparison_lossy_body(
            "今日｜民族観念［＃「民族観念」に傍点］と呼ぶ。悲憤｜慷慨《こうがい》も知悉《ちしつ》した",
        );

        assert_eq!(visible, "今日民族観念と呼ぶ。悲憤慷慨も知悉した");
    }
}
