package cn.wubo.sql.util;

import cn.wubo.sql.util.exception.StringException;

import java.util.Arrays;
import java.util.Optional;

public class StringUtils {

    private StringUtils() {
    }

    private static final String IS_EMPTY_MESSAGE = "参数%s不能为空！";

    public static void isEmpty(String message, String... strs) {
        Optional<Boolean> result = Arrays.stream(strs).map(str -> str == null || str.isEmpty()).filter(Boolean::booleanValue).findAny();
        if (result.isPresent()) throw new StringException(String.format(IS_EMPTY_MESSAGE, message));

    }

    public static void isEmpty(String message, String str) {
        if (str == null || str.isEmpty()) throw new StringException(String.format(IS_EMPTY_MESSAGE, message));
    }
}
