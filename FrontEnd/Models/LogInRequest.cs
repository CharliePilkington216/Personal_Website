using System.Text.Json.Serialization;

internal class LogInRequest
{
    [JsonPropertyName("email")]
    public string Email { private get; init; } = "";

    [JsonPropertyName("password")]
    public string Password { private get; init; } = "";
}