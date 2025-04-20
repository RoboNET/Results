using System.Diagnostics.CodeAnalysis;

namespace RoboNet.Results;

/// <summary>
/// Base result
/// </summary>
/// <param name="Errors">Errors</param>
public record Result(params BaseError[] Errors)
{
    /// <summary>
    /// Check if the result does not contain errors
    /// </summary>
    public virtual bool Ok => Errors is not { Length: > 0 };

    /// <summary>
    /// Create a successful result
    /// </summary>
    /// <returns>Successful result</returns>
    public static Result Success() => new();

    /// <summary>
    /// Create a successful result with data
    /// </summary>
    /// <param name="data">Result data</param>
    /// <typeparam name="T">Data type</typeparam>
    /// <returns>Successful result with data</returns>
    public static Result<T> Success<T>(T data) where T : notnull => new(data);

    /// <summary>
    /// Create a failed result
    /// </summary>
    /// <param name="errors">Errors</param>
    /// <returns>Result with errors</returns>
    public static Result Fail(BaseError[] errors) => new(errors);

    /// <summary>
    /// Create a failed result with data
    /// </summary>
    /// <param name="errors">Errors</param>
    /// <typeparam name="T">Data type</typeparam>
    /// <returns>Result with errors</returns>
    public static Result<T> Fail<T>(BaseError[] errors) where T : notnull => new(default, errors);

    public static implicit operator Result(BaseError error) => new(error);

    public static implicit operator Result(BaseError[] errors) => new(errors);
}

public record Result<T>(T? Data, params BaseError[] Errors) : Result(Errors) where T : notnull
{
    /// <summary>
    /// Get result data or throw an exception
    /// </summary>
    /// <returns>Result data</returns>
    /// <exception cref="InvalidOperationException">If the result is not OK</exception>
    [return: NotNullIfNotNull(nameof(Data))]
    public T Unwrap() => Ok ? Data : throw new InvalidOperationException();

    /// <summary>
    /// Check if the result does not contain errors and data is not null
    /// </summary>
    [MemberNotNullWhen(true, nameof(Data))]
    public override bool Ok => base.Ok && Data != null;

    public static implicit operator Result<T>(BaseError error) => new(default, error);

    public static implicit operator Result<T>(BaseError[] errors) => new(default, errors);

    public static implicit operator Result<T>(T data) => new(data);
}