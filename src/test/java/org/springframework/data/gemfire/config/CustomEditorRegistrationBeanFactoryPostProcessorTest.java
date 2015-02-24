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

import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.Test;
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
import org.springframework.data.gemfire.wan.OrderPolicyConverter;
import org.springframework.data.gemfire.wan.StartupPolicyConverter;
import org.springframework.data.gemfire.wan.StartupPolicyType;

import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.InterestPolicy;
import com.gemstone.gemfire.cache.InterestResultPolicy;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.util.Gateway;

/**
 * The CustomEditorRegistrationBeanFactoryPostProcessorTest class...
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.config.CustomEditorRegistrationBeanFactoryPostProcessor
 * @since 1.6.0
 */
@SuppressWarnings("deprecation")
public class CustomEditorRegistrationBeanFactoryPostProcessorTest {

	@Test
	public void testCustomEditorRegistration() {
		ConfigurableListableBeanFactory mockBeanFactory = mock(ConfigurableListableBeanFactory.class,
			"testCustomEditorRegistration.MockBeanFactory");

		new CustomEditorRegistrationBeanFactoryPostProcessor().postProcessBeanFactory(mockBeanFactory);

		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(EvictionAction.class),
			eq(EvictionActionConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(EvictionPolicyType.class),
			eq(EvictionPolicyConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(ExpirationAction.class),
			eq(ExpirationActionConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(IndexMaintenancePolicyType.class),
			eq(IndexMaintenancePolicyConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(IndexType.class), eq(IndexTypeConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(InterestPolicy.class),
			eq(InterestPolicyConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(InterestResultPolicy.class),
			eq(InterestResultPolicyConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(Scope.class), eq(ScopeConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(Gateway.OrderPolicy.class),
			eq(OrderPolicyConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(StartupPolicyType.class),
			eq(StartupPolicyConverter.class));
		verify(mockBeanFactory, times(1)).registerCustomEditor(eq(SubscriptionEvictionPolicy.class),
			eq(SubscriptionEvictionPolicyConverter.class));
	}

}
