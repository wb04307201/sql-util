package cn.wubo.sql.util.exception;

public class EntityWebException extends RuntimeException {

    public EntityWebException(String message) {
        super(message);
    }

    public EntityWebException(String message, Throwable cause) {
        super(message, cause);
    }
}
