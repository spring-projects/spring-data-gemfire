/*
 * Copyright 2011-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import java.util.Collection;

import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.RegionService;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.IndexExistsException;
import com.gemstone.gemfire.cache.query.IndexInvalidException;
import com.gemstone.gemfire.cache.query.IndexNameConflictException;
import com.gemstone.gemfire.cache.query.IndexType;
import com.gemstone.gemfire.cache.query.QueryService;
import com.gemstone.gemfire.cache.query.RegionNotFoundException;
import com.springsource.vfabric.licensing.log.Logger;

/**
 * Factory bean for easy declarative creation of GemFire Indexes.
 * 
 * @author Costin Leau
 */
public class IndexFactoryBean implements InitializingBean, BeanNameAware, FactoryBean<Index> {

	private Index index;
	private QueryService queryService;
	private String poolName;
	private RegionService cache;
	private String beanName;
	private String name, expression, from, imports;
	private IndexType type = IndexType.FUNCTIONAL;
	private boolean override = true;

	public void afterPropertiesSet() throws Exception {
		if (queryService == null) {
			if (cache != null) {
				queryService = cache.getQueryService();
			}
		}

		if (queryService != null && StringUtils.hasText(poolName)) {
			Pool pool = PoolManager.find(poolName);
			Assert.notNull(pool, "No pool named [" + poolName + "] found");
			queryService = pool.getQueryService();
		}

		Assert.notNull(queryService, "Query service required for index creation");
		Assert.hasText(expression, "Index expression is required");
		Assert.hasText(from, "Index from clause is required");

		String indexName = StringUtils.hasText(name) ? name : beanName;

		Assert.hasText(indexName, "Index bean id or name is required");

		index = createIndex(queryService, indexName);
	}

	private Index createIndex(QueryService queryService, String indexName) throws Exception  {
		Collection<Index> indexes = queryService.getIndexes();

		Index old = null;

		for (Index index : indexes) {
			if (indexName.equals(index.getName())) {
				if (!override) {
					return index;
				}
				old = index;
				break;
			}
		}

		if (old != null) {
			// compare indices
			if (from.equals(old.getFromClause()) && expression.equals(old.getIndexedExpression())
					&& type.equals(old.getType())) {
				return index;
			}
		}

		Index index = null;
		try {
		if (StringUtils.hasText(imports)) {
			index = queryService.createIndex(indexName, type, expression, from, imports);
		}
		else {
			index = queryService.createIndex(indexName, type, expression, from);
		}
		
	} catch (IndexExistsException e) {
		 // This is ok
	}  

		return index;
	}

	public Index getObject() {
		return index;
	}

	public Class<?> getObjectType() {
		return (index != null ? index.getClass() : Index.class);
	}

	public boolean isSingleton() {
		return true;
	}

	/**
	 * Sets the underlying cache used for creating indexes.
	 * 
	 * @param cache cache used for creating indexes.
	 */
	public void setCache(RegionService cache) {
		this.queryService = cache.getQueryService();
	}

	/**
	 * Sets the query service used for creating indexes.
	 * 
	 * @param service query service used for creating indexes.
	 */
	public void setQueryService(QueryService service) {
		this.queryService = service;
	}

	/**
	 * Sets the beanName of the {@link Pool} used for used for creating indexes.
	 * 
	 * @param poolName the beanName of the pool used for creating indexes.
	 */
	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * @param name the name to set
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @param expression the expression to set
	 */
	public void setExpression(String expression) {
		this.expression = expression;
	}

	/**
	 * @param from the from to set
	 */
	public void setFrom(String from) {
		this.from = from;
	}

	/**
	 * @param imports the imports to set
	 */
	public void setImports(String imports) {
		this.imports = imports;
	}

	/**
	 * @param type the type to set
	 */
	public void setType(IndexType type) {
		this.type = type;
	}

	/**
	 * @param override the override to set
	 */
	public void setOverride(boolean override) {
		this.override = override;
	}
}