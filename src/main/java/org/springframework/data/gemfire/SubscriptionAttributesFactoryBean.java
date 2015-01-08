/*
 * Copyright 2010-2013 the original author or authors.
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

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

import com.gemstone.gemfire.cache.InterestPolicy;
import com.gemstone.gemfire.cache.SubscriptionAttributes;

/**
 * Simple utility class used for defining nested factory-method like definitions w/o polluting the container with useless beans.
 * 
 * @author Lyndon Adams
 * @author John Blum
 * @since 1.3.0
 */
public class SubscriptionAttributesFactoryBean implements FactoryBean<SubscriptionAttributes>, InitializingBean {

	private SubscriptionAttributes subscriptionAttributes;

	private SubscriptionType type;

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.InitializingBean#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		subscriptionAttributes = new SubscriptionAttributes(getPolicy());
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

	public void setType(SubscriptionType type) {
		this.type = type;
	}

	public SubscriptionType getType() {
		return (type != null ? type : SubscriptionType.DEFAULT);
	}

	public void setPolicy(InterestPolicy policy) {
		setType(SubscriptionType.valueOf(policy));
	}

	public InterestPolicy getPolicy() {
		return getType().getInterestPolicy();
	}

}
