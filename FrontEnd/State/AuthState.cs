internal class AuthState
{
    public string? AccessToken { get; private set; }

    public void SetToken(string token)
    {
        AccessToken = token;
    }

    public void Clear()
    {
        AccessToken = null;
    }
}