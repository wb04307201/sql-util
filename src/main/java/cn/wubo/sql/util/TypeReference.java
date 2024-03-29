package cn.wubo.sql.util;

import cn.wubo.sql.util.cache.MemoryCache;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

public class TypeReference<T> {
    public final Class<T> clazz;

    /**
     * 构造方法，初始化泛型类的实际类型.
     * <p>
     * 此构造方法通过反射获取父类的泛型参数类型，并将其存储在成员变量clazz中。
     * 使用MemoryCache缓存获取或存储实际类型，提高性能。
     */
    protected TypeReference() {
        // 获取当前子类继承的父类（即TypeReference类）的泛型参数的实际类型
        Type superClass = getClass().getGenericSuperclass();
        Type tempType = ((ParameterizedType) superClass).getActualTypeArguments()[0];

        // 尝试从缓存中获取对应的Class对象
        Class<T> tempClazz = (Class<T>) MemoryCache.getClass(tempType);

        // 若缓存中不存在，则根据Type类型动态转换为Class对象
        if (tempClazz == null) {
            // 如果tempType是Class类型，则直接转换为Class对象
            if (tempType.toString().startsWith("class")) {
                tempClazz = (Class<T>) tempType;
            }
            // 否则tempType可能是ParameterizedType类型，获取其原始类型
            else {
                tempClazz = (Class<T>) ((ParameterizedType) tempType).getRawType();
            }

            // 将获取到的Class对象存入缓存
            MemoryCache.putClass(tempType, tempClazz);

            // 再次从缓存中获取以确保正确存储
            tempClazz = (Class<T>) MemoryCache.getClass(tempType);
        }

        // 最后将实际类型赋值给成员变量
        this.clazz = tempClazz;
    }
}
