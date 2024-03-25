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

    /**
     * 返回两个字符串中的非空非空字符串。如果第一个字符串为空或为空字符串，则返回第二个字符串；否则返回第一个字符串。
     *
     * @param str1 第一个字符串参数。
     * @param str2 第二个字符串参数。
     * @return 非空的字符串参数，优先返回str1。
     */
    public static String defaultValue(String str1, String str2) {
        // 判断第一个字符串是否为空，如果为空则返回第二个字符串，否则返回第一个字符串
        if (str1 == null || str1.isEmpty()) return str2;
        else return str1;
    }

}
