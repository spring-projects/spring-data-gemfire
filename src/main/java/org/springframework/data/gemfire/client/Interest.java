/*
 * Copyright 2010-2011 the original author or authors.
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

package org.springframework.data.gemfire.client;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.core.Constants;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.InterestResultPolicy;

/**
 * Basic holder class for registering an interest. Useful for configuring Gemfire caches through XML
 * and or JavaBeans means.
 * 
 * @author Costin Leau
 * @author Gary Russell
 */
public class Interest<K> implements InitializingBean {

	private static final Constants constants = new Constants(InterestResultPolicy.class);

	private K key;
	private InterestResultPolicy policy = InterestResultPolicy.DEFAULT;
	private boolean durable = false;
	private boolean receiveValues = false;

	public Interest() {
	}

	public Interest(K key) {
		this(key, InterestResultPolicy.DEFAULT, false, false);
	}

	public Interest(K key, InterestResultPolicy policy) {
		this(key, policy, false, false);
	}

	public Interest(K key, String policy) {
		this(key, policy, false, false);
	}

	public Interest(K key, String policy, boolean durable) {
		this(key, policy, durable, false);
	}

	public Interest(K key, String policy, boolean durable, boolean receiveValues) {
		this.key = key;
		this.policy = (InterestResultPolicy) constants.asObject(policy);
		this.durable = durable;
		this.receiveValues = receiveValues;
		afterPropertiesSet();
	}

	public Interest(K key, InterestResultPolicy policy, boolean durable, boolean receiveValues) {
		this.key = key;
		this.policy = policy;
		this.durable = durable;
		this.receiveValues = receiveValues;
		afterPropertiesSet();
	}


	public void afterPropertiesSet() {
		Assert.notNull(key, "a non-null key is required");
	}

	/**
	 * Returns the key of interest.
	 * 
	 * @return the key
	 */
	protected K getKey() {
		return key;
	}

	/**
	 * Sets the key of interest.
	 * 
	 * @param key the key to set
	 */
	public void setKey(K key) {
		this.key = key;
	}

	/**
	 * Returns the interest policy.
	 * 
	 * @return the policy
	 */
	protected InterestResultPolicy getPolicy() {
		return policy;
	}

	/**
	 * Sets the interest policy. The argument is set as an Object
	 * to be able to accept both InterestResultType instances but also
	 * Strings (for XML configurations).
	 * 
	 * @param policy the policy to set
	 */
	public void setPolicy(Object policy) {
		if (policy instanceof InterestResultPolicy) {
			this.policy = (InterestResultPolicy) policy;
		}
		else {
			if (policy instanceof String) {
				this.policy = (InterestResultPolicy) constants.asObject((String) policy);
			}
			else {
				throw new IllegalArgumentException("Unknown argument type for property 'policy'" + policy);
			}
		}
	}

	/**
	 * Returns the interest durability.
	 * 
	 * @return the durable
	 */
	protected boolean isDurable() {
		return durable;
	}

	/**
	 * Sets the interest durability.
	 * 
	 * @param durable the durable to set
	 */
	public void setDurable(boolean durable) {
		this.durable = durable;
	}

	/**
	 * Returns whether values are returned in events.
	 *
	 * @return the receiveValues
	 */
	protected boolean isReceiveValues() {
		return receiveValues;
	}

	/**
	 * Sets whether values are returned in events.
	 *
	 * @param receiveValues the receiveValues to set
	 */
	public void setReceiveValues(boolean receiveValues) {
		this.receiveValues = receiveValues;
	}
}