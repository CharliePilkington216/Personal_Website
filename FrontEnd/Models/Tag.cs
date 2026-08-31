using System.Text.Json.Serialization;

internal class Tag
{
    [JsonPropertyName("name")]
        public string Name { get; set; } = "";

    public Tag(string name)
    {
        Name = name;
    }
}