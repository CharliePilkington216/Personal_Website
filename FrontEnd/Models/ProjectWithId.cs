using System.Text.Json.Serialization;

internal class ProjectWithId
{
    [JsonPropertyName("project_id")]
    public string ProjectId { get; init; } = "";

    [JsonPropertyName("project")]
    public Project Project { get; init; } = new Project("", "", "", [], new DateTimeOffset());
}