package cn.wubo.sql.util.entity;

import lombok.Data;

@Data
public class DataTableEntity {
    //查询出的ReslutSet中的字段数量
    private int columnCount = 0;
    //字段名称数组
    private String[] columnNames;
    //字段类型数组
    private int[] columnTypes;

    public DataTableEntity() {
        this(0);
    }

    public DataTableEntity(int columnCount) {
        this.columnCount = columnCount;
        this.columnNames = new String[columnCount];
        this.columnTypes = new int[columnCount];
    }

    /**
     * 获取第index个字段名称，如果index字段不存在，则抛出ArrayIndexOutOfBoundsException异常
     *
     * @param index 第index个字段的索引值
     * @return 第index个字段的名称
     * @throws ArrayIndexOutOfBoundsException 如果index大于字段计数，则抛出异常
     */
    public String getColumnName(int index) {
        // 检查index是否小于等于字段计数
        if (index <= this.columnCount) return this.columnNames[index];
        else throw new ArrayIndexOutOfBoundsException();
    }


    /**
     * 设置第index个字段名称，如果index字段不存在，则抛出ArrayIndexOutOfBoundsException异常
     *
     * @param columnName 字段名称
     * @param index      index值
     */
    public void setColumnName(String columnName, int index) {
        // 检查index是否小于等于字段计数
        if (index <= this.columnCount) this.columnNames[index] = columnName;
        else throw new ArrayIndexOutOfBoundsException();
    }


    /**
     * 获取字段类型，不存在则抛出ArrayIndexOutOfBoundsException异常
     *
     * @param index 索引，表示要获取的字段的索引位置
     * @return 返回指定字段的类型
     */
    public int getColumnType(int index) {
        // 检查索引是否越界
        if (index <= this.columnCount) return this.columnTypes[index];
        else throw new ArrayIndexOutOfBoundsException();
    }


    /**
     * 获取字段类型，不存在则抛出ArrayIndexOutOfBoundsException异常
     *
     * @param columnType 字段类型
     * @param index      字段索引
     */
    public void setColumnType(int columnType, int index) {
        // 检查索引是否超出范围
        if (index <= this.columnCount) this.columnTypes[index] = columnType;
        else throw new ArrayIndexOutOfBoundsException();
    }

}
