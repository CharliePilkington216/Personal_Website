using System.Text.Json.Serialization;
using System.ComponentModel.DataAnnotations;

internal class TutoringRequest
{
    [JsonPropertyName("name")]
    [Required]
    public string Name { get; set; } = "";

    [JsonPropertyName("email")]
    [Required, EmailAddress]
    public string Email { get; set; } = "";

    [JsonPropertyName("tutoring_category")]
    [Required]
    public string TutoringCategory { get; set; } = "";

    [JsonPropertyName("description")]
    [Required]
    public string Description { get; set; } = "";
}