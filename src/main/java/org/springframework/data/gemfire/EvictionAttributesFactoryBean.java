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

import com.gemstone.gemfire.cache.EvictionAction;
import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.util.ObjectSizer;
import com.gemstone.gemfire.internal.cache.lru.LRUCapacityController;
import com.gemstone.gemfire.internal.cache.lru.MemLRUCapacityController;

/**
 * Simple utility class used for defining nested factory-method like definitions w/o polluting the container with useless beans.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see com.gemstone.gemfire.cache.EvictionAttributes
 * @see com.gemstone.gemfire.cache.util.ObjectSizer
 */
@SuppressWarnings("unused")
public class EvictionAttributesFactoryBean implements FactoryBean<EvictionAttributes>, InitializingBean {

	// TODO remove this reference to the GemFire internal class when the Gem team fixes the EvictionAttributes bug!!!
	protected static final int DEFAULT_LRU_MAXIMUM_ENTRIES = LRUCapacityController.DEFAULT_MAXIMUM_ENTRIES;

	protected static final int DEFAULT_MEMORY_MAXIMUM_SIZE = MemLRUCapacityController.DEFAULT_MAXIMUM_MEGABYTES;

	private EvictionAction action = null;

	private EvictionAttributes evictionAttributes;

	private EvictionType type = EvictionType.ENTRY_COUNT;

	private Integer threshold = null;

	private ObjectSizer objectSizer = null;

	public void afterPropertiesSet() {
		this.action = (this.action != null ? action : EvictionAction.DEFAULT_EVICTION_ACTION);
		evictionAttributes = createAttributes();
	}

	EvictionAttributes createAttributes() {
		switch (type) {
			case HEAP_PERCENTAGE:
				if (threshold != null) {
					throw new IllegalArgumentException(
						"The HEAP_PERCENTAGE (LRU_HEAP algorithm) does not support threshold (a.k.a. maximum)!");
				}
				return EvictionAttributes.createLRUHeapAttributes(objectSizer, action);
			case MEMORY_SIZE:
				return (threshold != null ? EvictionAttributes.createLRUMemoryAttributes(threshold, objectSizer, action)
					:  EvictionAttributes.createLRUMemoryAttributes(objectSizer, action));
			case ENTRY_COUNT:
			default:
				return (threshold != null ? EvictionAttributes.createLRUEntryAttributes(threshold, action)
					: EvictionAttributes.createLRUEntryAttributes(DEFAULT_LRU_MAXIMUM_ENTRIES, action));
		}
	}

	public EvictionAttributes getObject() {
		return evictionAttributes;
	}

	public Class<?> getObjectType() {
		return (evictionAttributes != null ? evictionAttributes.getClass() : EvictionAttributes.class);
	}

	public boolean isSingleton() {
		return true;
	}

	/**
	 * @return the action
	 */
	public EvictionAction getAction() {
		return action;
	}

	/**
	 * @param action the action to set
	 */
	public void setAction(EvictionAction action) {
		this.action = action;
	}

	/**
	 * @return the objectSizer
	 */
	public ObjectSizer getObjectSizer() {
		return objectSizer;
	}

	/**
	 * @param objectSizer the objectSizer to set
	 */
	public void setObjectSizer(ObjectSizer objectSizer) {
		this.objectSizer = objectSizer;
	}

	/**
	 * @return the threshold
	 */
	public Integer getThreshold() {
		return threshold;
	}

	/**
	 * @param threshold the threshold to set
	 */
	public void setThreshold(Integer threshold) {
		this.threshold = threshold;
	}

	/**
	 * @return the type
	 */
	public EvictionType getType() {
		return type;
	}

	/**
	 * @param type the type to set
	 */
	public void setType(EvictionType type) {
		this.type = type;
	}

}
