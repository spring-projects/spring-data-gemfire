/*
 * Copyright 2010-2020 the original author or authors.
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
package org.springframework.data.gemfire;

import java.util.Arrays;

import org.apache.geode.cache.AttributesFactory;
import org.apache.geode.cache.RegionAttributes;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.util.StringUtils;

/**
 * Spring-friendly bean for creating {@link RegionAttributes}. Eliminates the need of using a XML 'factory-method' tag.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.apache.geode.cache.AttributesFactory
 * @see org.apache.geode.cache.RegionAttributes
 */
@SuppressWarnings({ "unused" })
public class RegionAttributesFactoryBean<K, V> extends AttributesFactory<K, V>
		implements FactoryBean<RegionAttributes>, InitializingBean {

	private RegionAttributes<K, V> regionAttributes;

	@Override
	public void afterPropertiesSet() throws Exception {
		this.regionAttributes = super.create();
	}

	@Override
	public RegionAttributes<K, V> getObject() throws Exception {
		return this.regionAttributes;
	}

	@Override
	public Class<?> getObjectType() {

		return this.regionAttributes != null
			? this.regionAttributes.getClass()
			: RegionAttributes.class;
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	public void setAsyncEventQueueIds(String[] asyncEventQueueIds) {

		Arrays.stream(ArrayUtils.nullSafeArray(asyncEventQueueIds, String.class))
			.filter(StringUtils::hasText)
			.map(String::trim)
			.forEach(this::addAsyncEventQueueId);
	}

	public void setIndexUpdateType(IndexMaintenancePolicyType indexUpdateType) {
		indexUpdateType.setIndexMaintenance(this);
	}

	public void setGatewaySenderIds(String[] gatewaySenderIds) {

		Arrays.stream(ArrayUtils.nullSafeArray(gatewaySenderIds, String.class))
			.filter(StringUtils::hasText)
			.map(String::trim)
			.forEach(this::addGatewaySenderId);
	}
}
