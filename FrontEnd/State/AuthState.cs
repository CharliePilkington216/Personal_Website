internal class AuthState
{
    public string? AccessToken { get; private set; }

    public bool IsAuthorized => AccessToken is not null;

    public event Action? Changed;

    public void SetToken(string token)
    {
        AccessToken = token;
        Changed?.Invoke();
    }

    public void Clear()
    {
        AccessToken = null;
        Changed?.Invoke();
    }
}