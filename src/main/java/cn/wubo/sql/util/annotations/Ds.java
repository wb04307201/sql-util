package cn.wubo.sql.util.annotations;

/**
 * 用于定义数据源配置的注解，提供数据库连接的相关信息，如URL、用户名和密码。
 *
 * @Retention(RetentionPolicy.RUNTIME) 保留策略：在运行时保留注解，允许通过反射机制读取。
 * @Target(ElementType.ANNOTATION_TYPE) 注解目标：只能应用于其他注解类型。
 * @Documented 该注解将被包含在JavaDoc中，便于文档化和理解。
 */
public @interface Ds {

    /**
     * 数据库连接URL，遵循JDBC规范，如 `jdbc:mysql://localhost:3306/mydatabase`。
     *
     * @return 数据库连接URL字符串，默认为空字符串。
     */
    String url() default "";

    /**
     * 数据库用户名，用于登录数据库。
     *
     * @return 数据库用户名字符串，默认为空字符串。
     */
    String username() default "";

    /**
     * 数据库密码，与用户名配合使用，用于验证登录。
     *
     * @return 数据库密码字符串，默认为空字符串。
     */
    String passowrd() default "";
}

