package cn.wubo.sql.util.exception;

public class ConnectionPoolException extends RuntimeException{

    public ConnectionPoolException(String message) {
        super(message);
    }

    public ConnectionPoolException(Throwable cause) {
        super(cause);
    }

    public ConnectionPoolException(String message, Throwable cause) {
        super(message, cause);
    }
}
