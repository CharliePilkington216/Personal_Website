using System.Text.Json.Serialization;

internal class Project
{
    [JsonPropertyName("title")]
    public string Title { get; set; } = "";

    [JsonPropertyName("project_link")]
    public string ProjectLink { get; set; } = "";

    [JsonPropertyName("description")]
    public string Description { get; set; } = "";

    [JsonPropertyName("tags")]
    public string[] Tags { get; set; } = [];

    [JsonPropertyName("project_date")]
    public DateTimeOffset ProjectDate { get; set; }

    public Project(string title, string projectLink, string description, string[] tags, DateTimeOffset projectDate)
    {
        Title = title;
        ProjectLink = projectLink;
        Description = description;
        Tags = tags;
        ProjectDate = projectDate;
    }
}