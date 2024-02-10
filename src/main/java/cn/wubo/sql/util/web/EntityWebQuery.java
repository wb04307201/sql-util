package cn.wubo.sql.util.web;

import lombok.Data;

import java.util.HashMap;
import java.util.Map;

@Data
public class EntityWebQuery {
    private Map<String, Object> wheres = new HashMap<>();
}
