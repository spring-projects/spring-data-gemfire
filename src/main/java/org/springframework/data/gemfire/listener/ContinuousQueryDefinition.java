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

import java.util.function.Function;

import org.apache.geode.cache.query.CqAttributes;
import org.apache.geode.cache.query.CqAttributesFactory;
import org.apache.geode.cache.query.CqListener;
import org.apache.geode.cache.query.CqQuery;
import org.apache.shiro.util.StringUtils;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

/**
 * Class type for defining a {@link CqQuery}.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.InitializingBean
 */
@SuppressWarnings("unused")
public class ContinuousQueryDefinition implements InitializingBean {

	private final boolean durable;

	private final ContinuousQueryListener listener;

	private final String name;
	private final String query;

	public ContinuousQueryDefinition(String query, ContinuousQueryListener listener) {
		this(query, listener, false);
	}

	public ContinuousQueryDefinition(String query, ContinuousQueryListener listener, boolean durable) {
		this(null, query, listener, durable);
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

	/**
	 * Determines whether the CQ is durable.
	 *
	 * @return a boolean indicating if the CQ is durable.
	 */
	public boolean isDurable() {
		return this.durable;
	}

	/**
	 * Determines whether the CQ was named.
	 *
	 * @return a boolean value indicating whether the CQ is named.
	 * @see #getName()
	 */
	public boolean isNamed() {
		return StringUtils.hasText(getName());
	}

	/**
	 * Returns a reference to the {@link ContinuousQueryListener} that will process/handle CQ event notifications.
	 *
	 * @return the CQ listener registered with the CQ to handle CQ events.
	 */
	public ContinuousQueryListener getListener() {
		return this.listener;
	}

	/**
	 * Gets the {@link String name} of the CQ.
	 *
	 * @return the {@link String name} of the CQ.
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * Gets the {@link String query} executed by the CQ.
	 *
	 * @return the {@link String query} executed by the CQ.
	 */
	public String getQuery() {
		return this.query;
	}

	@Override
	public void afterPropertiesSet() {
		Assert.hasText(query, "Query is required");
		Assert.notNull(listener, "Listener is required");
	}

	public CqAttributes toCqAttributes(Function<ContinuousQueryListener, CqListener> listenerFunction) {

		CqAttributesFactory attributesFactory = new CqAttributesFactory();

		attributesFactory.addCqListener(listenerFunction.apply(getListener()));

		return attributesFactory.create();
	}
}
