package cn.wubo.sql.util.annotations;

/**
 * 定义编辑场景下下拉选项列表中的单个选项，包含选项值（value）和标签（label）。
 *
 * @Retention(RetentionPolicy.RUNTIME) 保留策略：在运行时保留注解，允许通过反射机制读取。
 * @Target(ElementType.ANNOTATION_TYPE) 注解目标：只能应用于其他注解类型。
 * @Documented 该注解将被包含在JavaDoc中，便于文档化和理解。
 */
public @interface Item {

    /**
     * 选项值，用于在提交数据时标识所选选项。通常为数据库中对应的唯一标识（如ID）。
     *
     * @return 选项值字符串。
     */
    String value();

    /**
     * 选项标签，用于在编辑界面显示给用户的可读文本。应简洁明了地描述该选项的含义。
     *
     * @return 选项标签字符串。
     */
    String label();
}
