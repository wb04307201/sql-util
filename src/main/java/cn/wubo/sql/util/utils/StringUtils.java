package cn.wubo.sql.util.utils;

import cn.wubo.sql.util.exception.StringException;

import java.util.Arrays;
import java.util.Optional;

public class StringUtils {

    private StringUtils() {
    }

    private static final String IS_EMPTY_MESSAGE = "参数%s不能为空！";

    /**
     * 检查传入的字符串数组中是否存在空字符串
     *
     * @param message 错误信息
     * @param strs    字符串数组
     * @throws StringException 如果存在空字符串，则抛出异常
     */
    public static void isEmpty(String message, String... strs) {
        Optional<Boolean> result = Arrays.stream(strs).map(str -> str == null || str.isEmpty()).filter(Boolean::booleanValue).findAny();
        if (result.isPresent()) throw new StringException(String.format(IS_EMPTY_MESSAGE, message));
    }

    /**
     * 检查传入的字符串是否为空
     *
     * @param message 错误信息
     * @param str     待检查的字符串
     * @throws StringException 如果字符串为空，则抛出异常
     */
    public static void isEmpty(String message, String str) {
        if (str == null || str.isEmpty()) throw new StringException(String.format(IS_EMPTY_MESSAGE, message));
    }

}
