package cn.wubo.sql.util;

import cn.wubo.sql.util.exception.SQLRuntimeException;

import java.util.Arrays;
import java.util.function.Function;
import java.util.function.Predicate;

public class FunctionUtils {

    private FunctionUtils() {
    }

    @SafeVarargs
    public static Boolean compileConditionOr(SQL.Where where, Predicate<SQL.Where>... predicates) {
        return Arrays.stream(predicates).anyMatch(item -> item.test(where));
    }

    @SafeVarargs
    public static Boolean compileConditionAnd(SQL.Where where, Predicate<SQL.Where>... predicates) {
        return Arrays.stream(predicates).allMatch(item -> item.test(where));
    }

    public static String buildCondition(SQL.Where where, Predicate<SQL.Where> predicate, Function<SQL.Where, String> function) {
        if (predicate.test(where)) return function.apply(where);
        else throw new SQLRuntimeException("recieving incomplete where condition in values invalid");
    }
}
