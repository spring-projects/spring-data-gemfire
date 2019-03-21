/*
 * Copyright 2017-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.annotation;

import java.util.Map;

import com.gemstone.gemfire.cache.GemFireCache;

import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.annotation.support.GemFireAsLastResourceConnectionAcquiringAspect;
import org.springframework.data.gemfire.config.annotation.support.GemFireAsLastResourceConnectionClosingAspect;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.util.Assert;

/**
 * The {@link GemFireAsLastResourceConfiguration} class is a Spring {@link Configuration @Configuration}
 * annotated class used to configure the GemFire "Last Resource" Spring Data GemFire {@link Aspect Aspects}.
 *
 * @author John Blum
 * @see org.aspectj.lang.annotation.Aspect
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.core.annotation.AnnotationAttributes
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.annotation.EnableGemFireAsLastResource
 * @see org.springframework.data.gemfire.config.annotation.support.GemFireAsLastResourceConnectionAcquiringAspect
 * @see org.springframework.data.gemfire.config.annotation.support.GemFireAsLastResourceConnectionClosingAspect
 * @see org.springframework.transaction.annotation.EnableTransactionManagement
 * @see com.gemstone.gemfire.cache.GemFireCache
 * @since 1.9.0
 */
@Configuration
@SuppressWarnings("unused")
public class GemFireAsLastResourceConfiguration implements ImportAware {

	private Integer enableTransactionManagementOrder;

	/* (non-Javadoc) */
	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {
		this.enableTransactionManagementOrder = resolveEnableTransactionManagementOrder(importMetadata);
	}

	/* (non-Javadoc) */
	@SuppressWarnings("all")
	protected int resolveEnableTransactionManagementOrder(AnnotationMetadata importMetadata) {

		AnnotationAttributes enableTransactionManagementAttributes =
			resolveEnableTransactionManagementAttributes(importMetadata);

		Integer order = enableTransactionManagementAttributes.getNumber("order");

		return validateOrder(order);
	}

	/* (non-Javadoc) */
	private Integer validateOrder(Integer order) {

		if (isInvalidOrder(order)) {
			throw new IllegalArgumentException(String.format(
				"The @%1$s(order) attribute value [%2$s] must be explicitly set to a value other than Integer.MAX_VALUE"
					+ " or Integer.MIN_VALUE", EnableTransactionManagement.class.getSimpleName(),
				String.valueOf(order)));
		}

		return order;
	}

	/* (non-Javadoc) */
	private boolean isInvalidOrder(Integer order) {
		return !isValidOrder(order);
	}

	/* (non-Javadoc) */
	private boolean isValidOrder(Integer order) {
		return !(order == null || order == Integer.MAX_VALUE || order == Integer.MIN_VALUE);
	}

	/* (non-Javadoc) */
	@SuppressWarnings("all")
	protected AnnotationAttributes resolveEnableTransactionManagementAttributes(
			AnnotationMetadata importMetadata) {

		Map<String, Object> enableTransactionManagementAttributes =
			importMetadata.getAnnotationAttributes(EnableTransactionManagement.class.getName());

		return validate(enableTransactionManagementAttributes);
	}

	/* (non-Javadoc) */
	private AnnotationAttributes validate(Map<String, Object> enableTransactionManagementAttributes) {

		Assert.state(enableTransactionManagementAttributes != null,
			String.format("The @%1$s annotation may only be used on a Spring application @%2$s class"
					+ " that is also annotated with @%3$s having an explicit [order] set",
				EnableGemFireAsLastResource.class.getSimpleName(), Configuration.class.getSimpleName(),
				EnableTransactionManagement.class.getSimpleName()));

		return AnnotationAttributes.fromMap(enableTransactionManagementAttributes);
	}

	/* (non-Javadoc) */
	protected Integer getEnableTransactionManagementOrder() {

		Assert.state(this.enableTransactionManagementOrder != null,
			String.format("The @%1$s(order) attribute was not properly set [%2$s]; Also, please make your"
					+ " Spring application @%3$s annotated class is annotated with both @%4$s and @%1$s",
				EnableTransactionManagement.class.getSimpleName(), this.enableTransactionManagementOrder,
				Configuration.class.getSimpleName(), EnableGemFireAsLastResource.class.getSimpleName()));

		return this.enableTransactionManagementOrder;
	}

	/* (non-Javadoc) */
	@Bean
	public Object gemfireCachePostProcessor(@Autowired(required = false) GemFireCache gemfireCache) {

		if (gemfireCache != null) {
			gemfireCache.setCopyOnRead(true);
		}

		return null;
	}

	/* (non-Javadoc) */
	@Bean
	public GemFireAsLastResourceConnectionAcquiringAspect gemfireJcaConnectionAcquiringAspect() {

		GemFireAsLastResourceConnectionAcquiringAspect connectionAcquiringAspect =
			new GemFireAsLastResourceConnectionAcquiringAspect();

		int order = (getEnableTransactionManagementOrder() + 1);

		connectionAcquiringAspect.setOrder(order);

		return connectionAcquiringAspect;
	}

	/* (non-Javadoc) */
	@Bean
	public GemFireAsLastResourceConnectionClosingAspect gemfireJcaConnectionClosingAspect() {

		GemFireAsLastResourceConnectionClosingAspect connectionClosingAspect =
			new GemFireAsLastResourceConnectionClosingAspect();

		int order = (getEnableTransactionManagementOrder() - 1);

		connectionClosingAspect.setOrder(order);

		return connectionClosingAspect;
	}
}
