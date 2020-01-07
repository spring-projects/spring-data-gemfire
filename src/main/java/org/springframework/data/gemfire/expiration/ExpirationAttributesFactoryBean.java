/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.expiration;

import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

/**
 * The ExpirationAttributesFactoryBean class is a Spring FactoryBean used to create Pivotal GemFire ExpirationAttributes
 * to specify Expiration policies for Region Time-to-Live (TTL) and Idle-Timeouts (TTI) as well as
 * Entry Time-to-Live (TTL) and Idle-Timeouts (TTI).
 *
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.apache.geode.cache.ExpirationAttributes
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class ExpirationAttributesFactoryBean implements FactoryBean<ExpirationAttributes>, InitializingBean {

	protected static final int DEFAULT_TIMEOUT = 0;

	protected static final ExpirationAction DEFAULT_EXPIRATION_ACTION =
		ExpirationActionType.DEFAULT.getExpirationAction();

	private ExpirationAction action;

	private ExpirationAttributes expirationAttributes;

	private Integer timeout;

	/* non-Javadoc */
	@Override
	public ExpirationAttributes getObject() throws Exception {
		return expirationAttributes;
	}

	/* non-Javadoc */
	@Override
	public Class<?> getObjectType() {
		return (expirationAttributes != null ? expirationAttributes.getClass() : ExpirationAttributes.class);
	}

	/* non-Javadoc */
	@Override
	public boolean isSingleton() {
		return true;
	}

	/**
	 * Sets the action to perform when a Region or an Entry expire.
	 *
	 * @param action the type of action to perform on expiration
	 * @see org.apache.geode.cache.ExpirationAction
	 */
	public void setAction(final ExpirationAction action) {
		this.action = action;
	}

	/**
	 * Gets the action to perform when a Region or an Entry expires.
	 *
	 * @return the type of action to perform on expiration.
	 * @see ExpirationActionType
	 * @see org.apache.geode.cache.ExpirationAttributes#getAction()
	 */
	public ExpirationAction getAction() {
		return (action != null ? action : DEFAULT_EXPIRATION_ACTION);
	}

	/**
	 * Sets the number of seconds before a Region or an Entry expires.
	 *
	 * @param timeout the number of seconds before a Region or an Entry expires.
	 */
	public void setTimeout(final Integer timeout) {
		this.timeout = timeout;
	}

	/**
	 * Gets the number of seconds before a Region or an Entry expires.
	 *
	 * @return the number of seconds before a Region or an Entry expires.
	 * @see org.apache.geode.cache.ExpirationAttributes#getTimeout()
	 */
	public int getTimeout() {
		return (timeout != null ? timeout : DEFAULT_TIMEOUT);
	}

	/**
	 * Initializes the Pivotal GemFire ExpirationAttributes produced by this factory.
	 *
	 * @throws Exception if the construction of the ExpirationAttributes was not successful.
	 * @see #getAction()
	 * @see #getTimeout()
	 * @see ExpirationActionType#getExpirationAction()
	 * @see org.apache.geode.cache.ExpirationAttributes
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		expirationAttributes = new ExpirationAttributes(getTimeout(), getAction());
	}

}
