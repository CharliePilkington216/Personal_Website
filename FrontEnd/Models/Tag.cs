using System.Text.Json.Serialization;

internal class Tag
{
    [JsonPropertyName("name")]
    public string Name { get; init; } = "";

    public Tag(string name)
    {
        Name = name;
    }
}