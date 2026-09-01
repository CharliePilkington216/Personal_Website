internal class PortfolioCache
{
    public Project[]? projectCache { get; set; }

    public Tag[]? tagCache { get; set; }

    public ProjectWithId[]? adminProjectCache { get; set; }

    public TagWithId[]? adminTagCache { get; set; }
}