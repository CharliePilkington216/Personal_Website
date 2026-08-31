using System.Text.Json.Serialization;
using System.ComponentModel.DataAnnotations;

internal class LogInRequest
{
    [JsonPropertyName("email")]
    [Required, EmailAddress]
    public string Email { get; set; } = "";

    [JsonPropertyName("password")]
    [Required]
    public string Password { get; set; } = "";
}