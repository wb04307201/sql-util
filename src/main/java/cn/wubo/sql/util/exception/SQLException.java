package cn.wubo.sql.util.exception;

public class SQLException extends RuntimeException{

    public SQLException(String message) {
        super(message);
    }

    public SQLException(Throwable cause) {
        super(cause);
    }
}
