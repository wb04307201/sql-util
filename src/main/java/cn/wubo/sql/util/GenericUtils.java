package cn.wubo.sql.util;

import java.lang.reflect.ParameterizedType;

/**
 * 泛型工具类
 *
 * @param <T> 泛型
 */
public class GenericUtils<T> {
    private Class<T> clazz;

    public GenericUtils() {
        this.clazz = (Class<T>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
    }

    public Class<T> getClassType() {
        return clazz;
    }
}
