package cn.wubo.sql.util.exception;

public class SQLRuntimeException extends RuntimeException {

    public SQLRuntimeException(String message) {
        super(message);
    }

    public SQLRuntimeException(Throwable cause) {
        super(cause);
    }
}
