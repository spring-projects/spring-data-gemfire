/*
 * Copyright 2010-2018 the original author or authors.
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

import org.apache.geode.cache.InterestPolicy;
import org.apache.geode.cache.SubscriptionAttributes;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

/**
 * The SubscriptionAttributesFactoryBean class is a Spring FactoryBean used for defining and constructing
 * a GemFire SubscriptionAttributes object, which determines the Subscription policy used by Regions to
 * declared their data interests.
 *
 * @author Lyndon Adams
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.apache.geode.cache.InterestPolicy
 * @see org.apache.geode.cache.SubscriptionAttributes
 * @since 1.3.0
 */
public class SubscriptionAttributesFactoryBean implements FactoryBean<SubscriptionAttributes>, InitializingBean {

	private InterestPolicy interestPolicy;

	private SubscriptionAttributes subscriptionAttributes;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.InitializingBean#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		subscriptionAttributes = new SubscriptionAttributes(getInterestPolicy());
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 */
	@Override
	public SubscriptionAttributes getObject() throws Exception {
		return subscriptionAttributes;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#getObjectType()
	 */
	@Override
	public Class<?> getObjectType() {
		return (subscriptionAttributes != null ? subscriptionAttributes.getClass() : SubscriptionAttributes.class);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#isSingleton()
	 */
	@Override
	public boolean isSingleton() {
		return true;
	}

	/**
	 * Sets GemFire's InterestPolicy specified on the SubscriptionAttributes in order to define/declare
	 * the data interests and distribution of changes.
	 *
	 * @param interestPolicy the GemFire InterestsPolicy to set for Subscription.
	 * @see org.apache.geode.cache.InterestPolicy
	 * @see org.apache.geode.cache.SubscriptionAttributes#SubscriptionAttributes(org.apache.geode.cache.InterestPolicy)
	 */
	public void setInterestPolicy(final InterestPolicy interestPolicy) {
		this.interestPolicy = interestPolicy;
	}

	/**
	 * Gets GemFire's InterestPolicy specified on the SubscriptionAttributes which defines data interests
	 * and distribution of changes.
	 *
	 * @return the GemFire InterestsPolicy set for Subscription.
	 * @see org.apache.geode.cache.InterestPolicy
	 * @see org.apache.geode.cache.SubscriptionAttributes#getInterestPolicy()
	 */
	public InterestPolicy getInterestPolicy() {
		return (interestPolicy != null ? interestPolicy : InterestPolicy.DEFAULT);
	}

	/* non-Javadoc */
	@Deprecated
	public void setPolicy(InterestPolicy policy) {
		setInterestPolicy(policy);
	}

	/* non-Javadoc */
	@Deprecated
	public InterestPolicy getPolicy() {
		return getInterestPolicy();
	}

}
