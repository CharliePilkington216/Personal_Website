using System.Text.Json.Serialization;

internal class TagWithId
{
    [JsonPropertyName("tag_id")]
    public string TagId { get; init; } = "";

    [JsonPropertyName("tag")]
    public Tag Tag { get; init; } = new Tag("");
}