package cn.wubo.sql.util.annotations;

import cn.wubo.sql.util.enums.EditType;

/**
 * 用于定义字段在编辑场景下的行为和配置的注解，如是否显示、编辑类型、是否必填、占位提示文字、下拉选项列表等。
 *
 * @Retention(RetentionPolicy.RUNTIME) 保留策略：在运行时保留注解，允许通过反射机制读取。
 * @Target(ElementType.ANNOTATION_TYPE) 注解目标：只能应用于其他注解类型。
 * @Documented 该注解将被包含在JavaDoc中，便于文档化和理解。
 */
public @interface Edit {

    /**
     * 是否在编辑界面显示该字段。如果为false，该字段将被隐藏。
     *
     * @return 布尔值，true表示显示，false表示隐藏，默认为true。
     */
    boolean show() default true;

    /**
     * 编辑类型，指定该字段在编辑界面的展现形式和交互方式。
     *
     * @return 枚举值EditType，表示编辑类型，默认为EditType.TEXT（文本输入框）。
     */
    EditType type() default EditType.TEXT;

    /**
     * 是否为必填字段。如果为true，用户在提交数据时必须填写该字段。
     *
     * @return 布尔值，true表示必填，false表示非必填，默认为false。
     */
    boolean notNull() default false;

    /**
     * 占位提示文字，当字段为空时在编辑界面显示的提示信息。
     *
     * @return 占位提示文字字符串，默认为空字符串。
     */
    String placeholder() default "";

    /**
     * 下拉选项列表，用于提供该字段的预设值供用户选择。适用于EditType为SELECT或多选类型的编辑场景。
     *
     * @return Item数组，表示下拉选项列表，默认为空数组。
     */
    Item[] items() default {};

    /**
     * 是否启用搜索功能。对于某些编辑类型（如多选、树形选择等），开启搜索功能可以方便用户快速定位所需选项。
     *
     * @return 布尔值，true表示启用搜索，false表示禁用搜索，默认为true。
     */
    boolean search() default true;
}

