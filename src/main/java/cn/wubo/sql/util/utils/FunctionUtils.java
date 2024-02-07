package cn.wubo.sql.util.utils;

import cn.wubo.sql.util.SQL;
import cn.wubo.sql.util.exception.SQLRuntimeException;

import java.util.Arrays;
import java.util.function.Function;
import java.util.function.Predicate;

public class FunctionUtils {

    private FunctionUtils() {
    }

    /**
     * Compile a condition or statement.
     *
     * @param where      the SQL.Where object
     * @param predicates the predicates to be compiled
     * @return true if any of the predicates match the given SQL.Where object, otherwise false
     */
    @SafeVarargs
    public static Boolean compileConditionOr(SQL.Where where, Predicate<SQL.Where>... predicates) {
        return Arrays.stream(predicates).anyMatch(item -> item.test(where));
    }

    /**
     * Compile the condition and return the result.
     *
     * @param where      the SQL.Where object
     * @param predicates the array of Predicate<SQL.Where> objects
     * @return true if all predicates are satisfied, otherwise false
     */
    @SafeVarargs
    public static Boolean compileConditionAnd(SQL.Where where, Predicate<SQL.Where>... predicates) {
        return Arrays.stream(predicates).allMatch(item -> item.test(where));
    }

    /**
     * Build the condition based on the given SQL.Where object, Predicate<SQL.Where> object, and Function<SQL.Where, String> object.
     *
     * @param where     the SQL.Where object
     * @param predicate the Predicate<SQL.Where> object
     * @param function  the Function<SQL.Where, String> object
     * @return the built condition string
     * @throws SQLRuntimeException if the predicate is not satisfied
     */
    public static String buildCondition(SQL.Where where, Predicate<SQL.Where> predicate, Function<SQL.Where, String> function) {
        if (predicate.test(where)) return function.apply(where);
        else throw new SQLRuntimeException("recieving incomplete where condition in values invalid");
    }

}
