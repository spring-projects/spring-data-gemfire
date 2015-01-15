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
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.RegionService;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.IndexExistsException;
import com.gemstone.gemfire.cache.query.QueryService;

/**
 * Spring FactoryBean for easy declarative creation of GemFire Indexes.
 * 
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.FactoryBean
 * @see com.gemstone.gemfire.cache.RegionService
 * @see com.gemstone.gemfire.cache.query.Index
 * @see com.gemstone.gemfire.cache.query.QueryService
 * @since 1.0.0
 */
public class IndexFactoryBean implements InitializingBean, FactoryBean<Index>, BeanNameAware {

	private boolean override = true;

	private Index index;

	private IndexType indexType;

	private QueryService queryService;

	private RegionService cache;

	private String beanName;
	private String expression;
	private String from;
	private String imports;
	private String name;

	public void afterPropertiesSet() throws Exception {
		Assert.notNull(cache, "The GemFire Cache reference must not be null!");

		queryService = lookupQueryService();

		Assert.notNull(queryService, "A QueryService is required for Index creation!");
		Assert.hasText(expression, "The Index 'expression' is required!");
		Assert.hasText(from, "The Index 'from' clause (a Region's full-path) is required!");

		if (IndexType.isKey(indexType)) {
			Assert.isNull(imports, "The 'imports' property is not supported for a Key Index.");
		}

		String indexName = (StringUtils.hasText(name) ? name : beanName);

		Assert.hasText(indexName, "The Index bean id or name is required!");

		index = createIndex(queryService, indexName);
	}

	QueryService lookupQueryService() {
		return (queryService != null ? queryService
			: (cache instanceof ClientCache ? ((ClientCache) cache).getLocalQueryService()
				: cache.getQueryService()));
	}

	Index createIndex(QueryService queryService, String indexName) throws Exception {
		Index existingIndex = getExistingIndex(queryService, indexName);

		if (existingIndex != null) {
			if (override) {
				queryService.removeIndex(existingIndex);
			}
			else {
				return existingIndex;
			}
		}

		try {
			if (IndexType.isKey(indexType)) {
				return queryService.createKeyIndex(indexName, expression, from);
			}
			else if (IndexType.isHash(indexType)) {
				return createHashIndex(queryService, indexName, expression, from, imports);
			}
			else {
				return createFunctionalIndex(queryService, indexName, expression, from, imports);
			}
		}
		catch (IndexExistsException e) {
			return getExistingIndex(queryService, indexName);
		}
		catch (Exception e) {
			if (existingIndex != null) {
				Collection<Index> indexes = queryService.getIndexes();

				if (CollectionUtils.isEmpty(indexes) || !indexes.contains(existingIndex)) {
					queryService.getIndexes().add(existingIndex);
					return existingIndex;
				}
			}

			throw e;
		}
	}

	Index createFunctionalIndex(QueryService queryService, String indexName, String expression, String from,
			String imports) throws Exception {
		if (StringUtils.hasText(imports)) {
			return queryService.createIndex(indexName, expression, from, imports);
		}
		else {
			return queryService.createIndex(indexName, expression, from);
		}
	}

	Index createHashIndex(QueryService queryService, String indexName, String expression, String from,
			String imports) throws Exception {
		if (StringUtils.hasText(imports)) {
			return queryService.createHashIndex(indexName, expression, from, imports);
		}
		else {
			return queryService.createHashIndex(indexName, expression, from);
		}
	}

	Index getExistingIndex(QueryService queryService, String indexName) {
		for (Index index : queryService.getIndexes()) {
			if (index.getName().equalsIgnoreCase(indexName)) {
				return index;
			}
		}

		return null;
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
		this.cache = cache;
	}

	/**
	 * Sets the query service used for creating indexes.
	 * 
	 * @param service query service used for creating indexes.
	 */
	public void setQueryService(QueryService service) {
		this.queryService = service;
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
	public void setType(String type) {
		setType(IndexType.valueOfIgnoreCase(type));
	}

	/**
	 * Sets the type of GemFire Index to create.
	 *
	 * @param indexType the IndexType enumerated value indicating the type of GemFire Index
	 * that will be created by this Spring FactoryBean.
	 * @see org.springframework.data.gemfire.IndexType
	 */
	public void setType(IndexType indexType) {
		this.indexType = indexType;
	}

	/**
	 * @param override the override to set
	 */
	public void setOverride(boolean override) {
		this.override = override;
	}

}
