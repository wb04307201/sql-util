package cn.wubo.sql.util.annotations;

/**
 * 用于定义字段在查看（展示）场景下的行为和配置的注解，如是否显示、是否可排序、是否可导出、列宽、是否支持多语言、下拉选项列表等。
 *
 * @Retention(RetentionPolicy.RUNTIME) 保留策略：在运行时保留注解，允许通过反射机制读取。
 * @Target(ElementType.ANNOTATION_TYPE) 注解目标：只能应用于其他注解类型。
 * @Documented 该注解将被包含在JavaDoc中，便于文档化和理解。
 */
public @interface View {

    /**
     * 是否在查看界面显示该字段。如果为false，该字段将被隐藏。
     *
     * @return 布尔值，true表示显示，false表示隐藏，默认为true。
     */
    boolean show() default true;

    /**
     * 是否允许对该字段进行排序。如果为true，用户可以在查看界面对列进行升序或降序排列。
     *
     * @return 布尔值，true表示可排序，false表示不可排序，默认为true。
     */
    boolean sortable() default true;

    /**
     * 是否允许在导出数据时包含该字段。如果为true，该字段将出现在导出的CSV、Excel等文件中。
     *
     * @return 布尔值，true表示可导出，false表示不可导出，默认为true。
     */
    boolean exportable() default true;

    /**
     * 列宽，用于指定该字段在查看界面的显示宽度（单位：像素）。
     *
     * @return 列宽整数值，默认为200像素。
     */
    int width() default 200;

    /**
     * 是否支持多语言。如果为true，该字段的显示内容将根据当前系统的语言设置进行本地化翻译。
     *
     * @return 布尔值，true表示支持多语言，false表示不支持多语言，默认为false。
     */
    boolean translatable() default false;

    /**
     * 下拉选项列表，用于提供该字段的预设值供用户参考。适用于查看界面需要展示选项标签而非原始值的场景。
     *
     * @return Item数组，表示下拉选项列表，默认为空数组。
     */
    Item[] items() default {};
}

