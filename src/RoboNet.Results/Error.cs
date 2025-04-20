namespace RoboNet.Results;

public record Error : BaseError
{
    public Error(Enum error)
    {
        Code = Convert.ToInt32(error);
        Message = error.ToString();
    }
    
    public Error(int code, string message)
    {
        Code = code;
        Message = message;
    }

    public int Code { get; }

    public string Message { get; }

}