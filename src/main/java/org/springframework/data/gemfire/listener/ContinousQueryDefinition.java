/*
 * Copyright 2011 the original author or authors.
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

package org.springframework.data.gemfire.listener;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.query.CqQuery;

/**
 * Basic holder class for defining an {@link CqQuery}. Useful for configuring GemFire {@link CqQuery}s through XML
 * and or JavaBeans means.
 * 
 * @author Costin Leau
 */
public class ContinousQueryDefinition implements InitializingBean {

	private String name = null, query = null;
	private ContinuousQueryListener listener = null;
	private boolean durable = false;

	public ContinousQueryDefinition() {
	}

	public ContinousQueryDefinition(String query, ContinuousQueryListener listener) {
		this(query, listener, false);
	}

	public ContinousQueryDefinition(String query, ContinuousQueryListener listener, boolean durable) {
		this.query = query;
		this.listener = listener;
		this.durable = durable;
		afterPropertiesSet();
	}

	public ContinousQueryDefinition(String name, String query, ContinuousQueryListener listener) {
		this(name, query, listener, false);
	}

	public ContinousQueryDefinition(String name, String query, ContinuousQueryListener listener, boolean durable) {
		this.name = name;
		this.query = query;
		this.listener = listener;
		this.durable = durable;
		afterPropertiesSet();
	}

	public void afterPropertiesSet() {
		Assert.hasText(query, "a non-empty query is required");
		Assert.notNull(listener, "a non- null listener is required");
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return the query
	 */
	public String getQuery() {
		return query;
	}

	/**
	 * @return the listener
	 */
	public ContinuousQueryListener getListener() {
		return listener;
	}

	/**
	 * @return the durable
	 */
	public boolean isDurable() {
		return durable;
	}
}