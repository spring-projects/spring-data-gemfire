/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.data.gemfire.EvictionActionConverter;
import org.springframework.data.gemfire.EvictionPolicyConverter;
import org.springframework.data.gemfire.EvictionPolicyType;
import org.springframework.data.gemfire.ExpirationActionConverter;
import org.springframework.data.gemfire.IndexMaintenancePolicyConverter;
import org.springframework.data.gemfire.IndexMaintenancePolicyType;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.IndexTypeConverter;
import org.springframework.data.gemfire.InterestPolicyConverter;
import org.springframework.data.gemfire.ScopeConverter;
import org.springframework.data.gemfire.client.InterestResultPolicyConverter;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicyConverter;

import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.InterestPolicy;
import com.gemstone.gemfire.cache.InterestResultPolicy;
import com.gemstone.gemfire.cache.Scope;

/**
 * The CustomEditorRegistrationBeanFactoryPostProcessor class is a Spring BeanFactoryPostProcessor used to register
 * custom GemFire JavaBeans PropertyEditors and Spring Converters that are used to perform type conversions between
 * String-based configuration meta-data and actual GemFire or SDG defined enumerated types.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanFactoryPostProcessor
 * @since 1.6.0
 */
@SuppressWarnings("unused")
public class CustomEditorRegistrationBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

	@Override
	public void postProcessBeanFactory(final ConfigurableListableBeanFactory beanFactory) throws BeansException {
		beanFactory.registerCustomEditor(EvictionAction.class, EvictionActionConverter.class);
		beanFactory.registerCustomEditor(EvictionPolicyType.class, EvictionPolicyConverter.class);
		beanFactory.registerCustomEditor(ExpirationAction.class, ExpirationActionConverter.class);
		beanFactory.registerCustomEditor(IndexMaintenancePolicyType.class, IndexMaintenancePolicyConverter.class);
		beanFactory.registerCustomEditor(IndexType.class, IndexTypeConverter.class);
		beanFactory.registerCustomEditor(InterestPolicy.class, InterestPolicyConverter.class);
		beanFactory.registerCustomEditor(InterestResultPolicy.class, InterestResultPolicyConverter.class);
		beanFactory.registerCustomEditor(Scope.class, ScopeConverter.class);
		beanFactory.registerCustomEditor(SubscriptionEvictionPolicy.class, SubscriptionEvictionPolicyConverter.class);
	}

}
