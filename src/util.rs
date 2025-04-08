pub mod spans {
    use miette::SourceSpan;

    pub fn from_range(start: &SourceSpan, end: &SourceSpan) -> SourceSpan {
        let len = end.offset() - start.offset() + end.len();
        SourceSpan::new(start.offset().into(), len)
    }
    
    pub fn placeholder_from(span: &SourceSpan) -> SourceSpan {
        let offset = span.offset() + span.len();
        SourceSpan::new(offset.into(), 0)
    }
    
    pub fn earliest(spans: &[SourceSpan]) -> Option<SourceSpan> {
        spans
            .into_iter()
            .min_by_key(|span| span.offset())
            .cloned()
    }
}
