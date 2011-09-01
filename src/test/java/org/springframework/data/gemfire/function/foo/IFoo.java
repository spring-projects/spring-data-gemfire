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
package org.springframework.data.gemfire.function.foo;

import java.util.List;
import java.util.Map;

/**
 * @author David Turanski
 *
 */
public interface IFoo {

	public abstract Integer oneArg(String key);

	public abstract Integer twoArg(String akey, String bkey);

	public abstract List<Integer> collections(List<Integer> args);

	public abstract Map<String, Integer> getMapWithNoArgs();

}