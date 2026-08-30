using System.Text.Json.Serialization;

internal class TutoringRequest
{
    [JsonPropertyName("name")]
    public string Name { private get; init; } = "";

    [JsonPropertyName("email")]
    public string Email { private get; init; } = "";

    [JsonPropertyName("tutoring_category")]
    public string TutoringCategory { private get; init; } = "";

    [JsonPropertyName("description")]
    public string Description { private get; init; } = "";
}