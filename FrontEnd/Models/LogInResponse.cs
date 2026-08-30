using System.Text.Json.Serialization;

internal class LogInResponse
{
    [JsonPropertyName("token")]
    public string Token { get; init; } = "";
}