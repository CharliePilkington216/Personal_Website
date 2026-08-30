internal class AuthState
{
    public string? AccessToken { get; private set; }

    public bool IsAuthorized => AccessToken is null;

    public void SetToken(string token)
    {
        AccessToken = token;
    }

    public void Clear()
    {
        AccessToken = null;
    }
}