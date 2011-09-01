/*
 * Copyright 2002-2011 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

import com.gemstone.gemfire.cache.execute.Function;

/**
 * @author David Turanski
 *
 * @param <T>
 */
public interface GemfireFunctionOperations<T> {

	public abstract List<T> executeOnRegion(Function function, String regionId, Serializable... args);

	public abstract T executeOnRegionAndExtract(Function function, String regionId, Serializable... args);

	public abstract List<T> executeOnRegion(Function function, String regionId, Set<?> keys, Serializable... args);

	public abstract List<T> executeOnRegion(String functionId, String regionId, Serializable... args);

	public abstract List<T> executeOnRegion(String functionId, String regionId, Set<?> keys, Serializable... args);

	public abstract T executeOnRegion(String regionId, GemfireFunctionCallback<T> callback);

	public abstract List<T> executeOnServers(Function function, Serializable... args);

	public abstract List<T> executeOnServers(String functionId, Serializable... args);

	public abstract T executeOnServers(GemfireFunctionCallback<T> callback);

}