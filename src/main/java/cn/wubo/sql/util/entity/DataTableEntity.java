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
     * @param index
     * @return
     */
    public String getColumnName(int index) {
        if (index <= this.columnCount) {
            return this.columnNames[index];
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }

    /**
     * 设置第index个字段名称，如果index字段不存在，则抛出ArrayIndexOutOfBoundsException异常
     * @param columnName
     * @param index
     */
    public void setColumnName(String columnName, int index) {
        if (index <= this.columnCount) {
            this.columnNames[index] = columnName;
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }

    /**
     * 获取字段类型，不存在则抛出ArrayIndexOutOfBoundsException异常
     * @param index
     * @return
     */
    public int getColumnType(int index) {
        if (index <= this.columnCount) {
            return this.columnTypes[index];
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }

    /**
     * 获取字段类型，不存在则抛出ArrayIndexOutOfBoundsException异常
     * @param columnType
     * @param index
     */
    public void setColumnType(int columnType, int index) {
        if (index <= this.columnCount) {
            this.columnTypes[index] = columnType;
        } else {
            throw new ArrayIndexOutOfBoundsException();
        }
    }
}
