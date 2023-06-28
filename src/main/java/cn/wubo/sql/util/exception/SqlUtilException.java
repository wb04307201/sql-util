package cn.wubo.sql.util.exception;

public class SqlUtilException extends RuntimeException{

    public SqlUtilException(String message) {
        super(message);
    }

    public SqlUtilException(Throwable cause) {
        super(cause);
    }
}
