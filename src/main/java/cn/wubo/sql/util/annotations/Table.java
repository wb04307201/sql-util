package cn.wubo.sql.util.annotations;


import java.lang.annotation.*;

/**
 * 用于标记实体类，提供与数据库表相关的元信息。这些信息在运行时可被反射访问，并用于数据操作或框架集成。
 *
 * @Retention(RetentionPolicy.RUNTIME) 保留策略：在运行时保留注解，允许通过反射机制读取。
 * @Target(ElementType.TYPE) 注解目标：只能应用于类级别。
 * @Documented 该注解将被包含在JavaDoc中，便于文档化和理解。
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
@Documented
public @interface Table {

    /**
     * 表名，对应数据库中的实际表名。
     *
     * @return 表名字符串。
     */
    String value();

    /**
     * 表描述，提供关于该表的简短说明。
     *
     * @return 表描述字符串，默认为空字符串。
     */
    String desc() default "";

    /**
     * 数据源配置，关联一个特定的数据源用于与该表进行交互。
     *
     * @return 一个Ds注解实例，表示表所属的数据源，默认为未指定（即使用默认数据源）。
     */
    Ds ds() default @Ds();

    /**
     * 初始化标志，指示是否在应用启动时自动初始化与该表相关的数据结构或操作。
     *
     * @return 布尔值，true表示需要自动初始化，false表示不需要，默认为true。
     */
    boolean init() default true;
}

