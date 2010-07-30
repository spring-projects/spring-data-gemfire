/*
 * Copyright 2010 the original author or authors.
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

import com.gemstone.gemfire.cache.PartitionAttributes;
import com.gemstone.gemfire.cache.PartitionResolver;

/**
 * Spring-friendly bean for creating {@link PartitionAttributes}. Eliminates the need of using
 * a XML 'factory-method' tag and allows the attributes properties to be set directly.
 * 
 * @author Costin Leau
 */
@SuppressWarnings("unchecked")
public class PartitionAttributesFactory implements FactoryBean<PartitionAttributes> {

	private com.gemstone.gemfire.cache.PartitionAttributesFactory paf = new com.gemstone.gemfire.cache.PartitionAttributesFactory();

	public PartitionAttributes getObject() throws Exception {
		return paf.create();
	}

	public Class<?> getObjectType() {
		return PartitionAttributes.class;
	}

	public boolean isSingleton() {
		return false;
	}


	public void setColocatedWith(String colocatedRegionFullPath) {
		paf.setColocatedWith(colocatedRegionFullPath);
	}


	public void setLocalMaxMemory(int mb) {
		paf.setLocalMaxMemory(mb);
	}


	public void setPartitionResolver(PartitionResolver resolver) {
		paf.setPartitionResolver(resolver);
	}


	public void setRecoveryDelay(long recoveryDelay) {
		paf.setRecoveryDelay(recoveryDelay);
	}


	public void setRedundantCopies(int redundantCopies) {
		paf.setRedundantCopies(redundantCopies);
	}


	public void setStartupRecoveryDelay(long startupRecoveryDelay) {
		paf.setStartupRecoveryDelay(startupRecoveryDelay);
	}


	public void setTotalMaxMemory(long mb) {
		paf.setTotalMaxMemory(mb);
	}


	public void setTotalNumBuckets(int numBuckets) {
		paf.setTotalNumBuckets(numBuckets);
	}
}