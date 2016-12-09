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

import java.util.List;

import com.gemstone.gemfire.cache.FixedPartitionAttributes;
import com.gemstone.gemfire.cache.PartitionAttributes;
import com.gemstone.gemfire.cache.PartitionAttributesFactory;
import com.gemstone.gemfire.cache.PartitionResolver;
import com.gemstone.gemfire.cache.partition.PartitionListener;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.util.CollectionUtils;

/**
 * Spring {@link FactoryBean} for creating {@link PartitionAttributes}.
 *
 * Eliminates the need to use a XML 'factory-method' tag and allows the attributes properties to be set directly.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see com.gemstone.gemfire.cache.FixedPartitionAttributes
 * @see com.gemstone.gemfire.cache.PartitionAttributes
 * @see com.gemstone.gemfire.cache.PartitionAttributesFactory
 * @see com.gemstone.gemfire.cache.PartitionResolver
 * @see com.gemstone.gemfire.cache.partition.PartitionListener
 */
@SuppressWarnings({ "rawtypes", "unchecked", "unused" })
public class PartitionAttributesFactoryBean<K, V> implements FactoryBean<PartitionAttributes<K, V>>, InitializingBean {

	private List<PartitionListener> partitionListeners;

	private PartitionAttributes<K, V> partitionAttributes;

	private final PartitionAttributesFactory<K, V> partitionAttributesFactory =
		new PartitionAttributesFactory<K, V>();

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		for (PartitionListener listener : CollectionUtils.nullSafeList(partitionListeners)) {
			partitionAttributesFactory.addPartitionListener(listener);
		}

		this.partitionAttributes = partitionAttributesFactory.create();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public PartitionAttributes getObject() throws Exception {
		return this.partitionAttributes;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Class<?> getObjectType() {
		return (this.partitionAttributes != null ? this.partitionAttributes.getClass() : PartitionAttributes.class);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean isSingleton() {
		return false;
	}

	public void setColocatedWith(String collocatedWith) {
		partitionAttributesFactory.setColocatedWith(collocatedWith);
	}

	public void setFixedPartitionAttributes(List<FixedPartitionAttributes> fixedPartitionAttributes) {
		for (FixedPartitionAttributes fixedPartitionAttributesElement :
				CollectionUtils.nullSafeList(fixedPartitionAttributes)) {

			partitionAttributesFactory.addFixedPartitionAttributes(fixedPartitionAttributesElement);
		}
	}

	public void setLocalMaxMemory(int mb) {
		partitionAttributesFactory.setLocalMaxMemory(mb);
	}

	public void setPartitionListeners(List<PartitionListener> partitionListeners) {
		this.partitionListeners = partitionListeners;
	}

	public void setPartitionResolver(PartitionResolver resolver) {
		partitionAttributesFactory.setPartitionResolver(resolver);
	}

	public void setRecoveryDelay(long recoveryDelay) {
		partitionAttributesFactory.setRecoveryDelay(recoveryDelay);
	}

	public void setRedundantCopies(int redundantCopies) {
		partitionAttributesFactory.setRedundantCopies(redundantCopies);
	}

	public void setStartupRecoveryDelay(long startupRecoveryDelay) {
		partitionAttributesFactory.setStartupRecoveryDelay(startupRecoveryDelay);
	}

	public void setTotalMaxMemory(long mb) {
		partitionAttributesFactory.setTotalMaxMemory(mb);
	}

	public void setTotalNumBuckets(int numBuckets) {
		partitionAttributesFactory.setTotalNumBuckets(numBuckets);
	}
}
