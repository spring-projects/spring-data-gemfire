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

package org.springframework.data.gemfire.listener;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.query.CqQuery;

/**
 * Basic holder class for defining an {@link CqQuery}. Useful for configuring GemFire {@link CqQuery}s by mean of
 * XML or using JavaBeans.
 * 
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.InitializingBean
 */
@SuppressWarnings("unused")
public class ContinuousQueryDefinition implements InitializingBean {

	private boolean durable = false;

	private ContinuousQueryListener listener;

	private String name;
	private String query;

	public ContinuousQueryDefinition() {
	}

	public ContinuousQueryDefinition(String query, ContinuousQueryListener listener) {
		this(query, listener, false);
	}

	public ContinuousQueryDefinition(String query, ContinuousQueryListener listener, boolean durable) {
		this.query = query;
		this.listener = listener;
		this.durable = durable;
		afterPropertiesSet();
	}

	public ContinuousQueryDefinition(String name, String query, ContinuousQueryListener listener) {
		this(name, query, listener, false);
	}

	public ContinuousQueryDefinition(String name, String query, ContinuousQueryListener listener, boolean durable) {
		this.name = name;
		this.query = query;
		this.listener = listener;
		this.durable = durable;
		afterPropertiesSet();
	}

	public void afterPropertiesSet() {
		Assert.hasText(query, "A non-empty query is required.");
		Assert.notNull(listener, "A non-null listener is required.");
	}

	/**
	 * Determines whether the CQ is durable.
	 *
	 * @return a boolean indicating if the CQ is durable.
	 */
	public boolean isDurable() {
		return durable;
	}

	/**
	 * Gets the name for the CQ.
	 *
	 * @return a String name for the CQ.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Gets the query string that will be executed for the CQ.
	 *
	 * @return a String value with the query to be executed for the CQ.
	 */
	public String getQuery() {
		return query;
	}

	/**
	 * The CQ Listener receiving events and notifications with changes from the CQ.
	 *
	 * @return the listener to be registered for the CQ.
	 */
	public ContinuousQueryListener getListener() {
		return listener;
	}

}
