package cn.wubo.sql.util.exception;

public class ModelSqlException extends RuntimeException {
    public ModelSqlException(String message) {
        super(message);
    }

    public ModelSqlException(Throwable cause) {
        super(cause);
    }
}
