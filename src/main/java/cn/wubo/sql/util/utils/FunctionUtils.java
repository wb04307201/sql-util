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
     * 编译一个条件或语句。
     *
     * @param where      SQL.Where对象，代表SQL查询中的WHERE子句部分。
     * @param predicates 要编译的断言集合，每个断言都是一个Predicate<SQL.Where>函数式接口实例，用于测试给定的SQL.Where对象是否满足条件。
     * @return 如果任何断言匹配给定的SQL.Where对象，则返回true，否则返回false。
     */
    @SafeVarargs
    public static Boolean compileConditionOr(SQL.Where where, Predicate<SQL.Where>... predicates) {
        // 使用流处理传入的断言数组，并测试每个断言是否对给定的SQL.Where对象返回true
        return Arrays.stream(predicates).anyMatch(item -> item.test(where));
    }


    /**
     * 编译条件并返回结果。
     * <p>
     * 此方法接收一个 SQL.Where 对象和一个 Predicate<SQL.Where> 对象数组，通过对数组中的每个 Predicate 对象执行 test 方法，检查是否所有 Predicate 对象的条件都得到满足。
     * 如果所有 Predicate 对象的 test 方法返回 true，则此方法返回 true，否则返回 false。
     * </p>
     *
     * @param where      SQL.Where 对象，代表一个 SQL 查询中的 WHERE 子句部分。
     * @param predicates Predicate<SQL.Where> 对象数组，每个对象是一个对 SQL.Where 对象进行判断的条件。
     * @return 如果所有 predicates 条件都满足，则返回 true；否则返回 false。
     */
    @SafeVarargs
    public static Boolean compileConditionAnd(SQL.Where where, Predicate<SQL.Where>... predicates) {
        // 使用 Arrays.stream 将 predicates 数组转换成流，然后通过 allMatch 方法检查所有 Predicate 对象是否都返回 true
        return Arrays.stream(predicates).allMatch(item -> item.test(where));
    }


    /**
     * 根据给定的 SQL.Where 对象、Predicate<SQL.Where> 对象和 Function<SQL.Where, String> 对象构建条件字符串。
     *
     * @param where     SQL.Where 对象，代表一个 SQL 查询中的 WHERE 子句。
     * @param predicate Predicate<SQL.Where> 对象，用于测试 SQL.Where 对象是否满足某些条件。
     * @param function  Function<SQL.Where, String> 对象，一个函数接口，用于根据满足条件的 SQL.Where 对象生成条件字符串。
     * @return 返回构建好的条件字符串。
     * @throws SQLRuntimeException 如果给定的 Predicate<SQL.Where> 对象测试不通过，则抛出此异常。
     */
    public static String buildCondition(SQL.Where where, Predicate<SQL.Where> predicate, Function<SQL.Where, String> function) {
        // 根据 predicate 对 where 进行测试，如果满足条件，则应用 function 生成并返回条件字符串；否则抛出异常。
        if (predicate.test(where)) return function.apply(where);
        else throw new SQLRuntimeException("recieving incomplete where condition in values invalid");
    }

}
