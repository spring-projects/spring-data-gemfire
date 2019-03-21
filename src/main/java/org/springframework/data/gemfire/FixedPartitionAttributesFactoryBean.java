/*
 * Copyright 2012 the original author or authors.
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

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.FixedPartitionAttributes;

/**
 * @author David Turanski
 *
 */
public class FixedPartitionAttributesFactoryBean implements FactoryBean<FixedPartitionAttributes>, InitializingBean {
	private Boolean primary;
	private String partitionName;
	private Integer numBuckets;
	private FixedPartitionAttributes fixedPartitionAttributes;

	/**
	 * Sets whether this particular PARTITION Region is the primary (i.e. not secondary).
	 *
	 * @param primary a boolean value to indicate whether this PARTITION Region is the primary.
	 */
	public void setPrimary(boolean primary) {
		this.primary = primary;
	}

	/**
	 * @param partitionName the partitionName to set
	 */
	public void setPartitionName(String partitionName) {
		this.partitionName = partitionName;
	}

	/**
	 * @param numBuckets the numBuckets to set
	 */
	public void setNumBuckets(Integer numBuckets) {
		this.numBuckets = numBuckets;
	}

	
	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 */
	@Override
	public FixedPartitionAttributes getObject() throws Exception {
		return fixedPartitionAttributes;
	}

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#getObjectType()
	 */
	@Override
	public Class<?> getObjectType() {
		return FixedPartitionAttributes.class;
	}

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#isSingleton()
	 */
	@Override
	public boolean isSingleton() {
		return true;
	}

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.InitializingBean#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		Assert.hasText(partitionName,"partitionName cannot be empty or null");
		
		fixedPartitionAttributes = null;
		if (primary == null && numBuckets == null){
			fixedPartitionAttributes = FixedPartitionAttributes.createFixedPartition(partitionName);
		} else if (primary == null && numBuckets != null){
			fixedPartitionAttributes = FixedPartitionAttributes.createFixedPartition(partitionName, numBuckets);
		} else if (primary != null && numBuckets == null) {
			fixedPartitionAttributes = FixedPartitionAttributes.createFixedPartition(partitionName, primary);
		} else {
			fixedPartitionAttributes = FixedPartitionAttributes.createFixedPartition(partitionName,primary,numBuckets);
		}
	
	}
}
