package cn.wubo.sql.util.annotations;

import cn.wubo.sql.util.enums.ColumnType;

import java.lang.annotation.*;

/**
 * 用于标记实体类字段，提供与数据库表列相关的元信息。这些信息在运行时可被反射访问，并用于数据操作或框架集成。
 *
 * @Retention(RetentionPolicy.RUNTIME) 保留策略：在运行时保留注解，允许通过反射机制读取。
 * @Target(ElementType.FIELD) 注解目标：只能应用于字段级别。
 * @Documented 该注解将被包含在JavaDoc中，便于文档化和理解。
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.FIELD})
@Documented
public @interface Column {

    /**
     * 数据库列名，对应数据库表中的实际列名。
     *
     * @return 列名字符串。
     */
    String value();

    /**
     * 数据库列描述，提供关于该列的简短说明。
     *
     * @return 列描述字符串，默认为空字符串。
     */
    String desc() default "";

    /**
     * 数据库列类型，指定该字段在数据库中的数据类型。
     *
     * @return 枚举值ColumnType，表示列的数据库类型，默认为ColumnType.VARCHAR（字符串类型）。
     */
    ColumnType type() default ColumnType.VARCHAR;

    /**
     * 字符类型长度，仅当列类型为字符串类型时有效，用于限制字段的最大长度。
     *
     * @return 字符串类型字段的长度，默认为200。
     */
    int length() default 200;

    /**
     * 数值类型精度，仅当列类型为数值类型时有效，表示数值的总位数（整数位+小数位）。
     *
     * @return 数值类型字段的精度，默认为18。
     */
    int precision() default 18;

    /**
     * 数值类型小数位数，仅当列类型为数值类型时有效，表示数值的小数位数。
     *
     * @return 数值类型字段的小数位数，默认为2。
     */
    int scale() default 2;

    /**
     * 列顺序，用于指定在数据库表中显示或操作时，该列相对于其他列的位置。
     *
     * @return 列顺序整数值，默认为100。
     */
    int sort() default 100;

    /**
     * 视图相关配置，关联一个特定的视图定义用于控制该字段在展示层的行为。
     *
     * @return 一个View注解实例，表示字段的视图配置，默认为未指定（即使用默认视图配置）。
     */
    View view() default @View();

    /**
     * 编辑相关配置，关联一个特定的编辑定义用于控制该字段在编辑层的行为。
     *
     * @return 一个Edit注解实例，表示字段的编辑配置，默认为未指定（即使用默认编辑配置）。
     */
    Edit edit() default @Edit();
}

